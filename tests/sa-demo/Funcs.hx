class Funcs {
	static function add(a:Int, b:Int):Int {
		return a + b;
	}

	static function fact(n:Int):Int {
		if (n <= 1) {
			return 1;
		}
		return n * fact(n - 1);
	}

	static function main():Void {
		var s:Int = add(20, 22);
		var f:Int = fact(5);
		if (s == 42) {
			trace("add ok");
		} else {
			trace("add bad");
		}
		if (f == 120) {
			trace("fact ok");
		} else {
			trace("fact bad");
		}
	}
}
