SinOsc _osc => dac;
0.5 => _osc.gain;
SndBuf ding => dac;
"sounds/ding.wav" => ding.read;
ding.samples() => ding.pos;
SndBuf left => dac;
"sounds/left.wav" => left.read;
left.samples() => left.pos;
SndBuf right => dac;
"sounds/right.wav" => right.read;
right.samples() => right.pos;
SndBuf ding1 => dac;
"sounds/splash.wav" => ding1.read;
ding1.samples() => ding1.pos;
SndBuf left1 => dac;
"sounds/low-bongo.wav" => left1.read;
left1.samples() => left1.pos;
SndBuf right1 => dac;
"sounds/high-bongo.wav" => right1.read;
right1.samples() => right1.pos;
440 => _osc.freq;
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
0 => _osc.freq;
<<< "off" >>>;
<<< "off" >>>;
<<< "off" >>>;
