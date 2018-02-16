source("./tests/make_examples.R", echo = TRUE)

# print & format

f_reg
print(f_reg, digits = 3)
print(f_reg, digits = 2, show = 11)

f_irreg
print(f_irreg, digits = 2, n= 2, show = 6)
