



def first_non_repeating_letter(s):
    original_string=s
    lower_string=original_string.lower()
    for letter in original_string:
        lower_letter=letter.lower()
        if lower_string.count(lower_letter)==1:
            return(letter)
    return("")


print(first_non_repeating_letter("HaTCAT"))
