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
(fs::current_path() / ".." / ".." / "resources" / "sfz" / "Jeux14" / "000/003_Montre_8_Prestant_4.sfz").string().c_str(),
  0);
organ->GetEngineChannel()->LoadInstrument();

 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOn(67, 80, 0);
organ->GetEngineChannel()->SendNoteOn(62, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(43, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(67, 80, 0);
organ->GetEngineChannel()->SendNoteOff(62, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(43, 80, 0);
organ->GetEngineChannel()->SendNoteOn(69, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(48, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(69, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(48, 80, 0);
organ->GetEngineChannel()->SendNoteOn(69, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(54, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(69, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOff(54, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(70, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(55, 80, 0);
organ->GetEngineChannel()->SendNoteOn(43, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(70, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(55, 80, 0);
organ->GetEngineChannel()->SendNoteOff(43, 80, 0);
organ->GetEngineChannel()->SendNoteOn(75, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(55, 80, 0);
organ->GetEngineChannel()->SendNoteOn(48, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(75, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOff(55, 80, 0);
organ->GetEngineChannel()->SendNoteOff(48, 80, 0);
organ->GetEngineChannel()->SendNoteOn(74, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(46, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(74, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOff(46, 80, 0);
organ->GetEngineChannel()->SendNoteOn(75, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(55, 80, 0);
organ->GetEngineChannel()->SendNoteOn(51, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(75, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(55, 80, 0);
organ->GetEngineChannel()->SendNoteOff(51, 80, 0);
organ->GetEngineChannel()->SendNoteOn(77, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(56, 80, 0);
organ->GetEngineChannel()->SendNoteOn(53, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(500));
organ->GetEngineChannel()->SendNoteOff(77, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(56, 80, 0);
organ->GetEngineChannel()->SendNoteOff(53, 80, 0);
organ->GetEngineChannel()->SendNoteOn(78, 80, 0);
organ->GetEngineChannel()->SendNoteOn(60, 80, 0);
organ->GetEngineChannel()->SendNoteOn(57, 80, 0);
organ->GetEngineChannel()->SendNoteOn(50, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(78, 80, 0);
organ->GetEngineChannel()->SendNoteOff(60, 80, 0);
organ->GetEngineChannel()->SendNoteOff(57, 80, 0);
organ->GetEngineChannel()->SendNoteOff(50, 80, 0);
organ->GetEngineChannel()->SendNoteOn(79, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(58, 80, 0);
organ->GetEngineChannel()->SendNoteOn(43, 80, 0);
std::this_thread::sleep_for(std::chrono::milliseconds(1000));
organ->GetEngineChannel()->SendNoteOff(79, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(58, 80, 0);
organ->GetEngineChannel()->SendNoteOff(43, 80, 0);
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}
