depth1 = () -> {
	x = 5
	depth2 = () -> {
		x = 3
		y = 2
		depth3 = (a, b) -> {
			a + b
		}
	}
}
