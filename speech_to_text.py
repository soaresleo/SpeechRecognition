import speech_recognition as sr
from pydub import AudioSegment, silence
import pandas as pd
import os

#Wav file correction
def export_corrected_wav(file, to_path):
    audio = AudioSegment.from_file(file, format="wav")
    #audio.export(to_path + '/conv_audio.wav', format="wav", bitrate="32k")
    return audio

#Channels split
def split_channels(file):
    audio = AudioSegment.from_file(file, "mp3")
    ch_list = audio.split_to_mono()
    return ch_list[1]
    #for i, ch in enumerate(ch_list, start=1):
    #ch.export(to_path + "ch_"+ str(i) + ".wav", format="wav", bitrate="32k")
    
#Audio chunks generator
def export_chunks(chunks_list, path):
    for i, chunk in enumerate(chunks_list, start=1):
        if i < 10:
            full_path = f'{path}/chunk_000{str(i)}.wav'
        elif i < 100:
            full_path = f'{path}/chunk_00{str(i)}.wav'
        elif i < 1000:
            full_path = f'{path}/chunk_0{str(i)}.wav'
        else:
            full_path = f'{path}/chunk_{str(i)}.wav'
        chunk.export(full_path, format="wav", bitrate="32k")

#Speech to text conversion
def speech_to_text(file_path):
    os.mkdir('temp_chunks')
    audio = split_channels(file_path)
    chunks = silence.split_on_silence(audio,
                                      min_silence_len=1000,
                                      silence_thresh=audio.dBFS + 0.05 * audio.dBFS,
                                      keep_silence=1000,
                                      seek_step=1)
        
    export_chunks(chunks, 'temp_chunks')
    chunks_paths = os.listdir('temp_chunks') 
    text = ''
    for path in chunks_paths:
        print('temp_chunks' + '/' + path)
        audio = sr.AudioFile('temp_chunks' + '/' + path)
        r = sr.Recognizer()
        with audio as source:
            rec = r.record(source)
            try:
                text += r.recognize_google(rec, language='pt-BR') + ' '
            except:
                text += 'recog_failed '
    os.rmdir("temp_chunks")
    return text