
public class VarV implements Value {

	private String name;

	public VarV(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return "\"" + name + "\"";
	}
}
