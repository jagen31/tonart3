#include<iostream>
#include<chrono>
#include<thread>
#include<filesystem>
#include<linuxsampler/Sampler.h>
#include<linuxsampler/drivers/audio/AudioOutputDeviceFactory.h>

namespace fs = std::__fs::filesystem;

int main() {
    auto sampler = new LinuxSampler::Sampler();
    auto factory = new LinuxSampler::AudioOutputDeviceFactory();

    auto params = std::map<std::string, std::string>();
    params["BUFFERSIZE"] = "2048";
    params["BUFFERS"] = "4";
    auto device = factory->Create("COREAUDIO", params);
auto bourdon16 = sampler->AddSamplerChannel();
bourdon16->SetAudioOutputDevice(device);
bourdon16->SetEngineType("SFZ");
bourdon16->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/015_Bourdon_16.sfz").string().c_str(),
  0);
bourdon16->GetEngineChannel()->LoadInstrument();

auto nuit8 = sampler->AddSamplerChannel();
nuit8->SetAudioOutputDevice(device);
nuit8->SetEngineType("SFZ");
nuit8->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/108_Echo_Flutes.sfz").string().c_str(),
  0);
nuit8->GetEngineChannel()->LoadInstrument();

auto bourdon8 = sampler->AddSamplerChannel();
bourdon8->SetAudioOutputDevice(device);
bourdon8->SetEngineType("SFZ");
bourdon8->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/051_Gedackt_8.sfz").string().c_str(),
  0);
bourdon8->GetEngineChannel()->LoadInstrument();

auto oboe8 = sampler->AddSamplerChannel();
oboe8->SetAudioOutputDevice(device);
oboe8->SetEngineType("SFZ");
oboe8->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / ".." / "resources" / "sfz" / "Jeux14" / "001/053_Hautbois_8.sfz").string().c_str(),
  0);
oboe8->GetEngineChannel()->LoadInstrument();

 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
bourdon8->GetEngineChannel()->SendNoteOn(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(56, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(56, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(51, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(56, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(56, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(51, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(56, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(75, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(56, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(75, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(59, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(73, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(75, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(63, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(75, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(80, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(51, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(80, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(80, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(54, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(54, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(78, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(80, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(54, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(54, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(167));
nuit8->GetEngineChannel()->SendNoteOff(80, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(80, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(167));
bourdon8->GetEngineChannel()->SendNoteOn(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(59, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(78, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(76, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(59, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(63, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(76, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(76, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(51, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(78, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(75, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(52, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(52, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(52, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(52, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(75, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(56, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(73, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(75, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(56, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(56, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(63, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(75, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(63, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(63, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOn(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(51, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(78, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(78, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(75, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(51, 80, 0);
bourdon8->GetEngineChannel()->SendNoteOn(52, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOn(52, 80, 0);
nuit8->GetEngineChannel()->SendNoteOff(75, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(75, 80, 0);
nuit8->GetEngineChannel()->SendNoteOn(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(333));
bourdon8->GetEngineChannel()->SendNoteOff(52, 80, 0);
bourdon16->GetEngineChannel()->SendNoteOff(52, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1333));
nuit8->GetEngineChannel()->SendNoteOff(73, 80, 0);
oboe8->GetEngineChannel()->SendNoteOff(73, 80, 0);
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}
