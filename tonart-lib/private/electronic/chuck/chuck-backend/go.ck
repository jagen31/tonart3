public class Go {

  static PlayQueued @ currentPlayQueued;
  static time currentTargetTime;

  fun static void queueMe() {
    currentPlayQueued => now;
  }

  fun static void begin() {
    now => currentTargetTime;
  }

  fun static void advanceTime(dur d) {
    if (now > currentTargetTime) {
      Go.currentPlayQueued.broadcast();
    }

    new PlayQueued @=> Go.currentPlayQueued; 

    // catch up in case we fell behind!
    if (currentTargetTime < now) {
      now => currentTargetTime;
    }

    currentTargetTime + d => Go.currentTargetTime;
    waitAndPlay(currentPlayQueued);
  }

  fun static void waitAndPlay(PlayQueued playQueued) {
    <<< "waiting !" >>>;
    <<< "now:", now >>>;
    <<< "target:", currentTargetTime>>>;

    // just making sure
    if (currentTargetTime > now) {
      currentTargetTime => now;
    }
    <<< "playing !" >>>;
    playQueued.broadcast();
  }
}

new PlayQueued @=> Go.currentPlayQueued;
