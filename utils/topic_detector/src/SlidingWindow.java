import java.util.Iterator;
import java.util.NoSuchElementException;


public class SlidingWindow{
    public static class Message{
        public String username;
        public String chat;
        public Message(String username, String chat){
            this.username = username;
            this.chat = chat;
        }
    }
	private Message[] a;            // queue elements
    private int N = 0;           // number of elements on queue
    private int first = 0;       // index of first element of queue
    private int last  = 0;       // index of next available slot

    // cast needed since no generic array creation in Java
    public SlidingWindow(int capacity) {
        a = new Message[capacity];
    }

    public boolean isEmpty() { return N == 0; }
    public int size()        { return N;      }

    public void enqueue(String username, String chat) {
        if (N == a.length) { throw new RuntimeException("Ring buffer overflow"); }
        a[last] = new Message(username, chat);
        last = (last + 1) % a.length;     // wrap-around
        N++;
    }

    // remove the least recently added item - doesn't check for underflow
    public Message dequeue() {
        if (isEmpty()) { throw new RuntimeException("Ring buffer underflow"); }
        Message item = a[first];
        a[first] = null;                  // to help with garbage collection
        N--;
        first = (first + 1) % a.length;   // wrap-around
        return item;
    }

    public Iterator<Message> iterator() { return new RingBufferIterator(); }

    // an iterator, doesn't implement remove() since it's optional
    private class RingBufferIterator implements Iterator<Message> {
        private int i = 0;
        public boolean hasNext()  { return i < N;                               }
        public void remove()      { throw new UnsupportedOperationException();  }

        public Message next() {
            if (!hasNext()) throw new NoSuchElementException();
            return a[i++];
        }
    }
	public String getString(){
		String s = "";
		Iterator<Message> i = iterator();
		while(i.hasNext())
			s += i.next().chat + " ";
		return s.trim();
	}

    public String getString(String username){
		String s = "";
		Iterator<Message> i = iterator();
		while(i.hasNext()){
			Message m = i.next();
            if(m.username.equals(username))
                s += i.next().chat + " ";
        }
		return s.trim();
	}

    public String getStringWithout(String username){
		String s = "";
		Iterator<Message> i = iterator();
		while(i.hasNext()){
			Message m = i.next();
            if(!m.username.equals(username))
                s += i.next().chat + " ";
        }
		return s.trim();
	}
}
