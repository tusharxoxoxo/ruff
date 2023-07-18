use ruff_text_size::TextSize;

use ruff_python_whitespace::{SimpleTokenizer, Token, TokenKind};

/// Searches for the first non-trivia character in `range`.
///
/// The search skips over any whitespace and comments.
///
/// Returns `Some` if the range contains any non-trivia character. The first item is the absolute offset
/// of the character, the second item the non-trivia character.
///
/// Returns `None` if the range is empty or only contains trivia (whitespace or comments).
pub(crate) fn first_non_trivia_token(offset: TextSize, code: &str) -> Option<Token> {
    SimpleTokenizer::starts_at(offset, code)
        .skip_trivia()
        .next()
}

/// Returns the first non-trivia token right before `offset` or `None` if at the start of the file
/// or all preceding tokens are trivia tokens.
///
/// ## Notes
///
/// Prefer [`first_non_trivia_token`] whenever possible because reverse lookup is expensive because of comments.
pub(crate) fn first_non_trivia_token_rev(offset: TextSize, code: &str) -> Option<Token> {
    SimpleTokenizer::up_to(offset, code)
        .skip_trivia()
        .next_back()
}

/// Returns the number of newlines between `offset` and the first non whitespace character in the source code.
pub(crate) fn lines_before(offset: TextSize, code: &str) -> u32 {
    let tokens = SimpleTokenizer::up_to(offset, code);
    let mut newlines = 0u32;

    for token in tokens.rev() {
        match token.kind() {
            TokenKind::Newline => {
                newlines += 1;
            }
            TokenKind::Whitespace => {
                // ignore
            }
            _ => {
                break;
            }
        }
    }

    newlines
}

/// Counts the empty lines between `offset` and the first non-whitespace character.
pub(crate) fn lines_after(offset: TextSize, code: &str) -> u32 {
    let tokens = SimpleTokenizer::starts_at(offset, code);
    let mut newlines = 0u32;

    for token in tokens {
        match token.kind() {
            TokenKind::Newline => {
                newlines += 1;
            }
            TokenKind::Whitespace => {
                // ignore
            }
            _ => {
                break;
            }
        }
    }

    newlines
}

/// Returns the position after skipping any trailing trivia up to, but not including the newline character.
pub(crate) fn skip_trailing_trivia(offset: TextSize, code: &str) -> TextSize {
    let tokenizer = SimpleTokenizer::starts_at(offset, code);

    for token in tokenizer {
        match token.kind() {
            TokenKind::Whitespace | TokenKind::Comment | TokenKind::Continuation => {
                // No op
            }
            _ => {
                return token.start();
            }
        }
    }

    offset
}

#[cfg(test)]
mod tests {
    use ruff_text_size::TextSize;

    use crate::trivia::{lines_after, lines_before};

    #[test]
    fn lines_before_empty_string() {
        assert_eq!(lines_before(TextSize::new(0), ""), 0);
    }

    #[test]
    fn lines_before_in_the_middle_of_a_line() {
        assert_eq!(lines_before(TextSize::new(4), "a = 20"), 0);
    }

    #[test]
    fn lines_before_on_a_new_line() {
        assert_eq!(lines_before(TextSize::new(7), "a = 20\nb = 10"), 1);
    }

    #[test]
    fn lines_before_multiple_leading_newlines() {
        assert_eq!(lines_before(TextSize::new(9), "a = 20\n\r\nb = 10"), 2);
    }

    #[test]
    fn lines_before_with_comment_offset() {
        assert_eq!(lines_before(TextSize::new(8), "a = 20\n# a comment"), 0);
    }

    #[test]
    fn lines_before_with_trailing_comment() {
        assert_eq!(
            lines_before(TextSize::new(22), "a = 20 # some comment\nb = 10"),
            1
        );
    }

    #[test]
    fn lines_before_with_comment_only_line() {
        assert_eq!(
            lines_before(TextSize::new(22), "a = 20\n# some comment\nb = 10"),
            1
        );
    }

    #[test]
    fn lines_after_empty_string() {
        assert_eq!(lines_after(TextSize::new(0), ""), 0);
    }

    #[test]
    fn lines_after_in_the_middle_of_a_line() {
        assert_eq!(lines_after(TextSize::new(4), "a = 20"), 0);
    }

    #[test]
    fn lines_after_before_a_new_line() {
        assert_eq!(lines_after(TextSize::new(6), "a = 20\nb = 10"), 1);
    }

    #[test]
    fn lines_after_multiple_newlines() {
        assert_eq!(lines_after(TextSize::new(6), "a = 20\n\r\nb = 10"), 2);
    }

    #[test]
    fn lines_after_before_comment_offset() {
        assert_eq!(lines_after(TextSize::new(7), "a = 20 # a comment\n"), 0);
    }

    #[test]
    fn lines_after_with_comment_only_line() {
        assert_eq!(
            lines_after(TextSize::new(6), "a = 20\n# some comment\nb = 10"),
            1
        );
    }
}
