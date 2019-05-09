
public class IntV implements Value {

	private int value;

	public IntV(int i) {
		value = i;
	}

	public int getValue() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
