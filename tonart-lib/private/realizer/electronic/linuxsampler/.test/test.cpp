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
auto organ = sampler->AddSamplerChannel();
organ->SetAudioOutputDevice(device);
organ->SetEngineType("SFZ");
organ->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/000_Montre_8.sfz").string().c_str(),
  0);
organ->GetEngineChannel()->LoadInstrument();

auto trumpet = sampler->AddSamplerChannel();
trumpet->SetAudioOutputDevice(device);
trumpet->SetEngineType("SFZ");
trumpet->GetEngineChannel()->PrepareLoadInstrument(
(fs::current_path() / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/065_Quintadena_8.sfz").string().c_str(),
  0);
trumpet->GetEngineChannel()->LoadInstrument();

 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(86, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(86, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(88, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(88, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(83, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(83, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(66, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(66, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(85, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(85, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(68, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(68, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(71, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(71, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(73, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(73, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(80, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(80, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(76, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(76, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(86, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(52, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(59, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(86, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(81, 80, 0);
organ->GetEngineChannel()->SendNoteOff(52, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(59, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(81, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(78, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(78, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(69, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(69, 80, 0);
trumpet->GetEngineChannel()->SendNoteOn(74, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
trumpet->GetEngineChannel()->SendNoteOff(74, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}
