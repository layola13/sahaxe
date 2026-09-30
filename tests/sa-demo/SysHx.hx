class SysHx {
	static function main():Void {
		Sys.putEnv("HX_SA_T", "abc");
		var v:String = Sys.getEnv("HX_SA_T");
		if (v == "abc") {
			trace("env ok");
		} else {
			trace("env bad");
		}
		trace(Sys.getCwd());
		var t:Float = Sys.time();
		if (t > 0.0) {
			trace("time ok");
		} else {
			trace("time bad");
		}
		Sys.sleep(0.01);
		trace("sleep ok");
	}
}
