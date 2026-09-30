class StrStore {
	static function main():Void {
		var a:String = "foo";
		var b:String = "bar";
		var c:String = a + "-" + b;
		var d:String = Std.string(7);
		var e:String = c + d;
		trace(e);
		if (e == "foo-bar7") {
			trace("store ok");
		} else {
			trace("store bad");
		}
		e = "baz";
		if (e == "baz") {
			trace("reassign ok");
		} else {
			trace("reassign bad");
		}
	}
}
