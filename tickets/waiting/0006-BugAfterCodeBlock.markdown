Subject: two space line after a code block broke things

The document likely looked like this

	Text
	
		code block
	  EOF

This broke the parser. Need to try to reproduce it and nix the error, because it was
a big nasty stack trace.