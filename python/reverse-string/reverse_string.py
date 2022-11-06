def reverse(text):
    reversed = ""
    for i in text:
        reversed = i + reversed
    return reversed