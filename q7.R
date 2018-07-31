my_list <- list("first_element" = F,
                "second_element" = matrix(data = 1:6, nrow = 2, ncol = 3),
                "third_element" = 20:200)
my_list[1]
for (i in my_list)
   if (is.logical(i))
      print(i)
