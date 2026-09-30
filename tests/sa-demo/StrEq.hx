class StrEq {
	static function main():Void {
		var a:String = "hello";
		var b:String = "hello";
		var c:String = "world";
		if (a == b) {
			trace("eq ok");
		} else {
			trace("eq bad");
		}
		if (a != c) {
			trace("neq ok");
		} else {
			trace("neq bad");
		}
		var s:String = "red";
		switch (s) {
			case "red": trace("is red");
			case "green" | "blue": trace("is cool");
			default: trace("other");
		}
	}
}
