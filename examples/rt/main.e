cint = intn <- 32
cchar = intn <- 8
cptr = ptr

argv_t = cptr <- cptr <- cchar

main = (argc : cint, argv : argv_t) -> {
	0 : cint
}
