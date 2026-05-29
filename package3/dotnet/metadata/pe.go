// Package metadata implements a Go-native ECMA-335 CLI metadata reader.
// It parses .NET assembly DLL files without invoking the .NET runtime,
// extracting the public type/method/property/field surface for the MEP-68
// type-mapping and wrapper-synthesis passes.
//
// Spec reference: ECMA-335 6th edition (ISO/IEC 23271:2012), available at
// https://ecma-international.org/publications-and-standards/standards/ecma-335/
package metadata

import (
	"encoding/binary"
	"fmt"
	"io"
)

const (
	mzMagic      = 0x5A4D // "MZ"
	peMagic      = 0x4550 // "PE\0\0"
	pe32Magic    = 0x010B
	pe32PlusMagic = 0x020B
	cliHeaderDirIndex = 14
)

// PEFile holds the parsed PE binary fields needed to locate the CLI metadata.
type PEFile struct {
	Is64Bit        bool
	NumberOfSections uint16
	Sections       []SectionHeader
	CLIHeaderRVA   uint32
	CLIHeaderSize  uint32
}

// SectionHeader represents one PE section (name, virtual address, raw offset).
type SectionHeader struct {
	Name             [8]byte
	VirtualSize      uint32
	VirtualAddress   uint32
	SizeOfRawData    uint32
	PointerToRawData uint32
}

// ParsePE reads the PE header from r and returns the structural fields needed
// to locate the CLI metadata root. r must be positioned at the start of the
// file (offset 0). ParsePE does not read any section data; it only reads
// the headers.
func ParsePE(r io.ReadSeeker) (*PEFile, error) {
	// DOS header: magic at offset 0, PE offset at offset 0x3C.
	var magic uint16
	if err := binary.Read(r, binary.LittleEndian, &magic); err != nil {
		return nil, fmt.Errorf("metadata/pe: read DOS magic: %w", err)
	}
	if magic != mzMagic {
		return nil, fmt.Errorf("metadata/pe: not a PE file (magic 0x%04X)", magic)
	}
	if _, err := r.Seek(0x3C, io.SeekStart); err != nil {
		return nil, fmt.Errorf("metadata/pe: seek to PE offset: %w", err)
	}
	var peOffset uint32
	if err := binary.Read(r, binary.LittleEndian, &peOffset); err != nil {
		return nil, fmt.Errorf("metadata/pe: read PE offset: %w", err)
	}
	if _, err := r.Seek(int64(peOffset), io.SeekStart); err != nil {
		return nil, fmt.Errorf("metadata/pe: seek to PE header: %w", err)
	}
	var peSig uint32
	if err := binary.Read(r, binary.LittleEndian, &peSig); err != nil {
		return nil, fmt.Errorf("metadata/pe: read PE signature: %w", err)
	}
	if peSig != peMagic {
		return nil, fmt.Errorf("metadata/pe: invalid PE signature 0x%08X", peSig)
	}

	// COFF header (20 bytes).
	var coff struct {
		Machine              uint16
		NumberOfSections     uint16
		TimeDateStamp        uint32
		PointerToSymbolTable uint32
		NumberOfSymbols      uint32
		SizeOfOptionalHeader uint16
		Characteristics      uint16
	}
	if err := binary.Read(r, binary.LittleEndian, &coff); err != nil {
		return nil, fmt.Errorf("metadata/pe: read COFF header: %w", err)
	}

	// Optional header: magic determines PE32 vs PE32+.
	var optMagic uint16
	if err := binary.Read(r, binary.LittleEndian, &optMagic); err != nil {
		return nil, fmt.Errorf("metadata/pe: read optional header magic: %w", err)
	}
	is64 := optMagic == pe32PlusMagic
	if optMagic != pe32Magic && optMagic != pe32PlusMagic {
		return nil, fmt.Errorf("metadata/pe: unknown optional header magic 0x%04X", optMagic)
	}

	// Skip to the data directory. The data directory starts at a fixed offset
	// within the optional header depending on PE32 vs PE32+.
	// PE32:  optional header starts at current pos-2 (we already read magic);
	//        NumberOfRvaAndSizes is at offset 92 from start of optional header.
	// PE32+: NumberOfRvaAndSizes is at offset 108 from start of optional header.
	// Data directories immediately follow NumberOfRvaAndSizes.
	var skipBytes int64
	if is64 {
		skipBytes = 108 - 2 // already read 2 bytes (magic)
	} else {
		skipBytes = 92 - 2
	}
	if _, err := r.Seek(skipBytes, io.SeekCurrent); err != nil {
		return nil, fmt.Errorf("metadata/pe: seek to data directories: %w", err)
	}
	var numDataDirs uint32
	if err := binary.Read(r, binary.LittleEndian, &numDataDirs); err != nil {
		return nil, fmt.Errorf("metadata/pe: read NumberOfRvaAndSizes: %w", err)
	}
	if uint32(cliHeaderDirIndex) >= numDataDirs {
		return nil, fmt.Errorf("metadata/pe: no CLI header data directory (only %d dirs)", numDataDirs)
	}

	// Data directories are 8-byte (RVA uint32 + Size uint32) entries.
	// Skip to index 14 (CLI header).
	if _, err := r.Seek(int64(cliHeaderDirIndex)*8, io.SeekCurrent); err != nil {
		return nil, fmt.Errorf("metadata/pe: seek to CLI dir entry: %w", err)
	}
	var cliRVA, cliSize uint32
	if err := binary.Read(r, binary.LittleEndian, &cliRVA); err != nil {
		return nil, fmt.Errorf("metadata/pe: read CLI header RVA: %w", err)
	}
	if err := binary.Read(r, binary.LittleEndian, &cliSize); err != nil {
		return nil, fmt.Errorf("metadata/pe: read CLI header size: %w", err)
	}
	if cliRVA == 0 {
		return nil, fmt.Errorf("metadata/pe: not a .NET assembly (no CLI header)")
	}

	// Skip remaining data directory entries to reach section headers.
	remaining := int64(numDataDirs-cliHeaderDirIndex-1) * 8
	if _, err := r.Seek(remaining, io.SeekCurrent); err != nil {
		return nil, fmt.Errorf("metadata/pe: seek to section headers: %w", err)
	}

	// Read section headers.
	sections := make([]SectionHeader, coff.NumberOfSections)
	for i := range sections {
		if err := binary.Read(r, binary.LittleEndian, &sections[i]); err != nil {
			return nil, fmt.Errorf("metadata/pe: read section header %d: %w", i, err)
		}
	}

	return &PEFile{
		Is64Bit:          is64,
		NumberOfSections: coff.NumberOfSections,
		Sections:         sections,
		CLIHeaderRVA:     cliRVA,
		CLIHeaderSize:    cliSize,
	}, nil
}

// RVAToOffset converts a relative virtual address to a file offset using the
// section table.
func (pe *PEFile) RVAToOffset(rva uint32) (uint32, error) {
	for _, s := range pe.Sections {
		if rva >= s.VirtualAddress && rva < s.VirtualAddress+s.VirtualSize {
			return s.PointerToRawData + (rva - s.VirtualAddress), nil
		}
	}
	return 0, fmt.Errorf("metadata/pe: RVA 0x%08X not found in any section", rva)
}
