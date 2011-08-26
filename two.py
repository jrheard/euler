# fast and dirty

a = 0
b = 1
sum = 0
while True:
	term = a + b
	if term >= 4000000:
		print sum
		break

	if term % 2 == 0:
		sum += term

	a = b
	b = term
