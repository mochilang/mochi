/* @generated */
import XCTest
import SwiftTreeSitter
import TreeSitterErlang

final class TreeSitterErlangTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_erlang())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Erlang grammar")
    }
}
