class RegexHx {
	static function main():Void {
		var r = new EReg("a(b+)", "");
		if (r.match("xxabbyy")) {
			var m0:String = r.matched(0);
			var m1:String = r.matched(1);
			trace(m0);
			trace(m1);
			if (m0 == "abb" && m1 == "bb") {
				trace("regex ok");
			} else {
				trace("regex bad");
			}
		} else {
			trace("nomatch bad");
		}
		var ri = new EReg("ABC", "i");
		if (ri.match("xxabcxx")) {
			trace("casefold ok");
		} else {
			trace("casefold bad");
		}
	}
}
