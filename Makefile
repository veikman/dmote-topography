# GNU makefile. https://www.gnu.org/software/make/manual/make.html

example.dat:
	lein run --x-mu 72 --x-sigma 18 --y-mu 15 --y-sigma 28 > example.dat
