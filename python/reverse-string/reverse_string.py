def reverse(text):
    reversed = ""
    for i in text:
        reversed = i + reversed
    return reversed

def reverse_2(text= ""):
    return text[::-1]