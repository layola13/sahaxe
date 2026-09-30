class StrRet {
	static function greet(name:String):String {
		return "hi " + name;
	}

	static function main():Void {
		var m:String = greet("bo");
		trace(m);
		trace(greet("al"));
		if (greet("bo") == m) {
			trace("ret ok");
		} else {
			trace("ret bad");
		}
		if (m == "hi bo") {
			trace("stored ok");
		} else {
			trace("stored bad");
		}
	}
}
