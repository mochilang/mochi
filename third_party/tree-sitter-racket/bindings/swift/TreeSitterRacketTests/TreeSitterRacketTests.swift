import XCTest
import SwiftTreeSitter
import TreeSitterRacket

final class TreeSitterRacketTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_racket())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Racket grammar")
    }
}
