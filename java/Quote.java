import java.util.*;

public class Quote implements Instr {
	private Value value;

	public Quote (Value value) {
		this.value = value;
	}

	@Override
	public void execute(Config config) {
		config.setValue(v);
		config.nextInstruction();
	}
}
