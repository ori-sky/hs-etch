char = intn <- 8
ptr_char = ptr <- intn <- 8

kb = 1024 : ptr_char

f = () -> {
	(kb |+ 27) |= 42:char
}
