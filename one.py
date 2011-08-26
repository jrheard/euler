# verbose version
multiples = []
for i in range(1000):
	if i % 3 == 0 or i % 5 == 0:
		multiples.append(i)

print sum(multiples)













# one-liner version
print sum(i for i in range(1000) if i % 3 == 0 or i % 5 == 0)
