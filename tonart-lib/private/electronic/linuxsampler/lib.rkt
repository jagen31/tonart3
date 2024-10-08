#lang at-exp racket

(require 
  art art/timeline art/coordinate/instant art/coordinate/switch 
  tonart/private/lib tonart/private/electronic/lib 
  racket/runtime-path rsound rsound/envelope sf2-parser
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict))
(provide (all-defined-out))

;; create a c++ string containing a program which plays the score via linuxsampler
(define-syntax (define-composite-linuxsampler-realizer stx)
  (syntax-parse stx
    [(_ n:id {subrealizer:id ...})
     #'(begin
         (define-composite-realizer n {subrealizer ...} [] 
           (λ(clauses)
              #`(begin
                  (define-values (note-statements instruments)
                    (for/fold ([statements '()] [instruments (set)] [t 0] #:result (values (reverse statements) (set->list instruments)))
                              ([expr (list #,@clauses)])
                      (match-define (list t* instrument-name midi-instrument-name statement) expr)
                      (values 
                        (if (= t* t)
                          (cons statement statements)
                          (cons statement (cons (format "std::this_thread::sleep_for(std::chrono::milliseconds(~a));" (inexact->exact (round (* 1000 (- t* t))))) statements)))
                        (set-add instruments (cons instrument-name midi-instrument-name))
                        t*)))

                  @string-append|{
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
|@(string-join 
  (for/list ([iname instruments])
    @string-append|{
auto |@(car iname) = sampler->AddSamplerChannel();
|@(car iname)->SetAudioOutputDevice(device);
|@(car iname)->SetEngineType("SFZ");
|@(car iname)->GetEngineChannel()->PrepareLoadInstrument(
|@(format "(fs::current_path() / \"..\" / \"..\" / \"..\" / \"resources\" / \"sfz\" / \"Jeux14\" / \"~a.sfz\").string().c_str()," (cdr iname))
  0);
|@(car iname)->GetEngineChannel()->LoadInstrument();
  
}|) "\n")
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
|@(string-join note-statements "\n")
 std::this_thread::sleep_for(std::chrono::milliseconds(1000));
}
}|))))]))


(define-subrealizer linuxsampler-midi-subrealizer
  (λ(ctxt*)
    (println "running midi subperf")
    (define imap* (context-ref ctxt* #'instrument-map))
    (unless imap* (raise-syntax-error 'midi-subrealizer "no instrument map in context"))
    (define imap (syntax-parse imap* [(_ map ...) (syntax->datum #'(map ...))]))
    (define ctxt (sort ctxt* < 
      #:key (λ (stx) 
        (define inst (context-ref (get-id-ctxt stx) #'instant))
        (if inst
          (syntax-parse inst [(_ result) (syntax-e #'result)])
          0))))
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~literal full-midi} num:number velocity:number [instrument-name:id ...]) 
         (define instant (context-ref (get-id-ctxt stx) #'instant))
         (define switch (context-ref (get-id-ctxt stx) #'switch))
         (unless instant (raise-syntax-error 'midi-subrealizer (format "this realizer requires an instant for all midis, got: ~s" (un-@ stx)) stx))
         (unless switch (raise-syntax-error 'midi-subrealizer (format "this realizer requires a switch for all midis, got: ~s" (un-@ stx)) stx))
         (syntax-parse #`(#,instant #,switch)
           [((_ time) (_ on?))
            (define instrument-names (syntax->datum #'(instrument-name ...)))
            (append 
              (for/list ([instrument-name* instrument-names])
                #`(list 
                    (exact->inexact time)
                    #,(symbol->string instrument-name*)
                    #,(symbol->string (dict-ref imap instrument-name*))
                    (if on?
                      (format "~a->GetEngineChannel()->SendNoteOn(~a, ~a, 0);" '#,instrument-name* num velocity)
                      (format "~a->GetEngineChannel()->SendNoteOff(~a, ~a, 0);" '#,instrument-name* num velocity))))
              acc)])]
        [({~literal midi} _) (raise-syntax-error 'linuxsampler-midi-subrealizer "expected full midi objects, got a normal midi" stx)]
        [_ acc]))))


(define-composite-linuxsampler-realizer linuxsampler-realizer {linuxsampler-midi-subrealizer})
