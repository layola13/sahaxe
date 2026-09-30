class Greeter {
	public var prefix:String;

	public function new(prefix:String) {
		this.prefix = prefix;
	}

	public function greet(name:String):Void {
		trace("hi");
	}

	public function greet2(name:String):Void {
		var msg:String = name;
		if (msg == "bob") {
			trace("bob ok");
		} else {
			trace("bob bad");
		}
	}
}

class StrParam {
	static function shout(s:String):Void {
		trace(s);
	}

	static function main():Void {
		var g = new Greeter("hey");
		g.greet("bob");
		g.greet2("bob");
		shout("loud");
		var w:String = "world";
		shout(w);
	}
}
