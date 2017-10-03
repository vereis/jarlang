#Sample Erlang
Sample Erlang files and programs that should test the capabilities and accuracy of jrlscript.

##Notes
Notes about how erlang handles particular examples of data, syntax, etc.

###atoms
	are implicitly defined, (they can't be undefined)
	1048576 atom limit
	reserved words are (technically?) atoms
	true and false are supposedly atoms but comparisons act funny? investigate.

###integers
	can be arbitrarily big
	use bignum arithmetic
	https://stackoverflow.com/questions/39268564/is-there-a-size-limit-for-erlang-integers