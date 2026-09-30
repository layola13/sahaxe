import haxe.macro.Expr;
import haxe.macro.Context;

class Mac {
	public static macro function twice(e:Expr):Expr {
		return macro ($e + $e);
	}

	static function main():Void {
		var x:Int = 21;
		var y:Int = Mac.twice(x);
		if (y == 42) {
			trace("macro ok");
		} else {
			trace("macro bad");
		}
	}
}
