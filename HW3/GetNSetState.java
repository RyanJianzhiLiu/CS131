import java.util.concurrent.atomic.AtomicIntegerArray;
class GetNSetState implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSetState(byte[] v) {
	value = byte2AtomicInt(v);
	maxval = 127;
    }

    GetNSetState(byte[] v, byte m) {
	value = byte2AtomicInt(v);
	maxval = m;
    }

    private AtomicIntegerArray byte2AtomicInt(byte[] v) {
        int len = v.length;
        AtomicIntegerArray value = new AtomicIntegerArray(len);
        for (int i = 0; i < len; i++)
            value.set(i,v[i]);
	return value;
    }
    
    private byte[] atomicInt2Byte(AtomicIntegerArray v) {
	int len = v.length();
	byte[] value = new byte[len];
	for (int i = 0; i < len; i++)
	    value[i] = (byte)v.get(i);
	return value;
    }
    
    public int size() { return value.length(); }

    public byte[] current() { return atomicInt2Byte(value); }

    public boolean swap(int i, int j) {
	int vi = value.get(i);
	int vj = value.get(j);
	if (vi <= 0 || vj >= maxval) {
	    return false;
	}
	value.set(i,vi--);
	value.set(j,vj++);
	return true;
    }
}
