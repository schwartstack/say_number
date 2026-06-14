SCALE_WORDS = c(
    "",
    "thousand",
    "million",
    "billion",
    "trillion",
    "quadrillion",
    "quintillion",
    "sextillion",
    "septillion",
    "octillion",
    "nonillion",
    "decillion",
    "undecillion",
    "duodecillion",
    "tredecillion",
    "quattuordecillion",
    "quindecillion",
    "sexdecillion",
    "septendecillion",
    "octodecillion",
    "novemdecillion",
    "vigintillion"
)
TENS_PLACE_WORDS = c(
    "ten",
    "twenty",
    "thirty",
    "fourty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety"
)
ONES_PLACE_WORDS = c(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen"
)

#' Convert a Number to Its Spoken Representation
#'
#' Converts a numeric value into a character string containing its
#' English-language spoken representation. Whole numbers,
#' decimal values, negative, and positive numbers are supported.
#' Decimal values are rendered using the word "point" followed by the
#' spoken form of each digit in the decimal portion.
#'
#' @param n A numeric value to convert.
#'
#' @return A character string.
#'
#' @examples
#' say(42)
#' # "forty-two"
#'
#' say(-1234)
#' # "negative one thousand two hundred thirty-four"
#'
#' say(3.1415)
#' # "three point one four one five"
#'
#' say(0.007)
#' # "zero point zero zero seven"
say <- function(n) {
    string_number = as.character(n)

    whole_part = strsplit(string_number, "\\.")[[1]][1]
    whole_string = .say_whole_number(whole_part)

    decimal_part = strsplit(string_number, "\\.")[[1]][2]

    if (is.na(decimal_part)) {
        return(whole_string)
    }
    decimal_string = .say_decimal_part(decimal_part)

    return(paste(whole_string, "point", decimal_string))
}

.say_whole_number <- function(string_number) {
    if (is.na(string_number)) {
        return(NA)
    }
    if (string_number == "0") {
        return("zero")
    }
    if (grepl("^-", string_number)) {
        string = "negative"
        string_number = gsub("^-", "", string_number)
    } else {
        string = ""
    }
    string_digits = strsplit(string_number, "")[[1]]
    numeric_digits = as.numeric(string_digits)
    while (length(numeric_digits) %% 3 != 0) {
        numeric_digits = c(0, numeric_digits)
    }
    matrix_number = matrix(numeric_digits, ncol = 3, byrow = T)

    for (row_index in 1:nrow(matrix_number)) {
        if (matrix_number[row_index, 1] > 0) {
            string = paste(
                string,
                ONES_PLACE_WORDS[matrix_number[row_index, 1]],
                "hundred"
            )
        }
        if (matrix_number[row_index, 2] > 1) {
            string = paste(
                string,
                TENS_PLACE_WORDS[matrix_number[row_index, 2]],
                ONES_PLACE_WORDS[matrix_number[row_index, 3]]
            )
            string = paste(
                trimws(string),
                rev(SCALE_WORDS[1:nrow(matrix_number)])[row_index]
            )
        } else if (
            matrix_number[row_index, 2] == 0 &&
                matrix_number[row_index, 3] == 0
        ) {
            if (matrix_number[row_index, 1] > 0) {
                string = paste(
                    trimws(string),
                    rev(SCALE_WORDS[1:nrow(matrix_number)])[row_index]
                )
            }
        } else {
            string = paste(
                string,
                ONES_PLACE_WORDS[
                    10 *
                        matrix_number[row_index, 2] +
                        matrix_number[row_index, 3]
                ]
            )
            string = paste(
                trimws(string),
                rev(SCALE_WORDS[1:nrow(matrix_number)])[row_index]
            )
        }
    }
    return(trimws(string))
}

.say_decimal_part <- function(string_decimal_part) {
    paste(
        sapply(
            strsplit(string_decimal_part, "")[[1]],
            .say_whole_number
        ),
        collapse = " "
    )
}
