
public class BoolV implements Value {

	private boolean value;

	public BoolV(boolean value) {
		this.value = value;
	}

	public boolean getValue() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
