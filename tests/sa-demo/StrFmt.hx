typedef MyInt = Int;

class StrFmt {
	static function main():Void {
		var n:MyInt = 42;
		trace(Std.string(n));
		var who:String = "sa";
		trace("hello " + who + "!");
	}
}
