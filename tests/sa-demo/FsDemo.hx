import sys.io.File;
import sys.FileSystem;

class FsDemo {
	static function main():Void {
		var path:String = "/tmp/hx_sa_demo.txt";
		File.saveContent(path, "hello fs");
		if (FileSystem.exists(path)) {
			trace("exists ok");
		} else {
			trace("exists bad");
		}
		trace(File.getContent(path));
		trace(Sys.getEnv("PATH"));
	}
}
