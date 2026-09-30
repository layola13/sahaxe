class StrSplit {
	static function main():Void {
		var parts:Array<String> = "a,b,c".split(",");
		var one:Array<String> = "abc".split(",");
		var holes:Array<String> = "a,,b".split(",");
		var trail:Array<String> = "a,".split(",");
		var lit:Array<String> = ["x", "yy"];
		var sub:String = "abc".substr(1, 1);
		trace(parts[0]);
		trace(parts[2]);
		if (parts.length == 3 && parts[0] == "a" && parts[1] == "b" && parts[2] == "c") {
			trace("split ok");
		} else {
			trace("split bad");
		}
		if (one.length == 1 && one[0] == "abc") {
			trace("single ok");
		} else {
			trace("single bad");
		}
		if (holes.length == 3 && holes[1] == "" && trail.length == 2 && trail[1] == "") {
			trace("holes ok");
		} else {
			trace("holes bad");
		}
		if (lit.length == 2 && lit[0] == "x" && lit[1] == "yy") {
			trace("lit ok");
		} else {
			trace("lit bad");
		}
		if (sub == "b") {
			trace("litsub ok");
		} else {
			trace("litsub bad");
		}
	}
}
