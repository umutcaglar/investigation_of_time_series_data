replace_funf <- function(input_vector, initialVal, finalVal)
{
  if(length(initialVal) != length(finalVal))
    {stop("length initial val should be equal to final val")}
  if(length(initialVal)!=length(unique(initialVal)))
    {stop("there should not be any repeats in initialVal")}
  
  levels_vector <- levels(factor(input_vector))
  locations = initialVal %in% levels_vector

  part2 = setdiff(levels_vector, initialVal)
  ordered_leves_vector = c(initialVal[locations], part2)
  ordered_final_vector = c(finalVal[locations], part2)
  
  input_vector %>%
    factor(., levels = ordered_leves_vector) %>%
    as.numeric(.) -> input_vector_n
  
  output_vector = ordered_final_vector[input_vector_n]
  
  return(output_vector)
}