SndBuf ding => dac;
"sounds/ding.wav" => ding.read;
SndBuf left => dac;
"sounds/left.wav" => left.read;
SndBuf right => dac;
"sounds/right.wav" => right.read;
SndBuf ding1 => dac;
"sounds/splash.wav" => ding1.read;
SndBuf left1 => dac;
"sounds/low-bongo.wav" => left1.read;
SndBuf right1 => dac;
"sounds/high-bongo.wav" => right1.read;
0 => ding.pos;
0 => left.pos;
0 => right.pos;
2000::ms => now;
<<< "off" >>>;
0 => right.pos;
1000::ms => now;
<<< "off" >>>;
0 => left.pos;
1000::ms => now;
<<< "off" >>>;
0 => ding.pos;
<<< "off" >>>;
0 => right.pos;
2000::ms => now;
<<< "off" >>>;
0 => left.pos;
<<< "off" >>>;
0 => right.pos;
2000::ms => now;
<<< "off" >>>;
0 => ding.pos;
<<< "off" >>>;
0 => right.pos;
1000::ms => now;
<<< "off" >>>;
0 => left.pos;
1000::ms => now;
<<< "off" >>>;
0 => right.pos;
2000::ms => now;
<<< "off" >>>;
<<< "off" >>>;
<<< "off" >>>;
0 => ding1.pos;
0 => left1.pos;
0 => right1.pos;
2000::ms => now;
<<< "off" >>>;
0 => right1.pos;
1000::ms => now;
<<< "off" >>>;
0 => left1.pos;
1000::ms => now;
<<< "off" >>>;
0 => ding1.pos;
<<< "off" >>>;
0 => right1.pos;
2000::ms => now;
<<< "off" >>>;
0 => left1.pos;
<<< "off" >>>;
0 => right1.pos;
2000::ms => now;
<<< "off" >>>;
0 => ding1.pos;
<<< "off" >>>;
0 => right1.pos;
1000::ms => now;
<<< "off" >>>;
0 => left1.pos;
1000::ms => now;
<<< "off" >>>;
0 => right1.pos;
2000::ms => now;
<<< "off" >>>;
<<< "off" >>>;
<<< "off" >>>;
