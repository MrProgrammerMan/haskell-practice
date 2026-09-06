
product [] = 1
product (n:ns) = n * Main.product ns

double x = x + x
quadruple x = double (double x)
quadrupleAlt = double . double
factorial n = Main.product [1..n]