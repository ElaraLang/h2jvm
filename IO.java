import java.util.function.Supplier;
class IO<T> {
    private final Supplier<T> run;

    public IO(Supplier<T> run) {
        this.run = run;
    }

    public void run() {
        this.run.get();
    }

    public static IO<Void> println(String s) {
        return new IO<>(() -> {
        System.out.println(s);
        return null;}
        );
    }

  
}

class B {
      public static IO<Void> println2(String s) {
        return IO.println("h");
    }
}
