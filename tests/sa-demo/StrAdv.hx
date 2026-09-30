class StrAdv {
	static function main():Void {
		var s:String = "hello, world";
		var i:Int = s.indexOf(",");
		var j:Int = s.indexOf("o", 5);
		var t:String = s.substr(7, 5);
		var u:String = s.toUpperCase();
		var lo:String = "ABC".toLowerCase();
		var c:String = s.charAt(1);
		var n:Int = s.length;
		trace(t);
		trace(u);
		trace(c);
		if (i == 5 && j == 8 && n == 12 && t == "world" && u == "HELLO, WORLD" && lo == "abc" && c == "e") {
			trace("stradv ok");
		} else {
			trace("stradv bad");
		}
	}
}
