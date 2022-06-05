//! Utility functions for matching unicode characters
use unicode_general_category::{get_general_category, GeneralCategory};

/// Checks if a character is a base character or a modifier. A character is considered a modifier
/// if it is in one of the following unicode classes:
/// - Lm (Letter, Modifier)
/// - Mc (Mark, Spacing Combining)
/// - Me (Mark, Enclosing)
/// - Mn (Mark, Non-Spacing)
/// - Sk (Symbol, Modifier)
/// Or, if it is a superscript or subscript
pub fn is_modifier(c: char) -> bool {
    matches!(c,
        // check against superscript 1, 2, and 3
        '\u{b9}' | '\u{b2}' | '\u{b3}'
        // check against superscripts and subscripts block
        | '\u{2070}'..='\u{209f}'
    ) ||
        // otherwise, check if c is in the named classes
        matches!(get_general_category(c),
            GeneralCategory::ModifierLetter
            | GeneralCategory::SpacingMark
            | GeneralCategory::EnclosingMark
            | GeneralCategory::NonspacingMark
            | GeneralCategory::ModifierSymbol
        )
}

/// Checks if a character is a modifier combining two characters
pub fn is_combining_double(c: char) -> bool {
    ('\u{035c}'..='\u{0362}').contains(&c)
}
