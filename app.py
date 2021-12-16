import speech_recognition as sr
from pydub import AudioSegment, silence
import pandas as pd
import os
import shutil
import time
import json
import traceback

# Wav file correction
def export_corrected_wav(file):
    audio = AudioSegment.from_file(file, format="wav")
    return audio

# Channels split
def split_channels(audio):
    ch_list = audio.split_to_mono()
    return ch_list[1]

def split_on_silence(mono_audio):
    return silence.split_on_silence(mono_audio,
                             min_silence_len=1000,
                             silence_thresh=mono_audio.dBFS + 0.05 * mono_audio.dBFS,
                             keep_silence=1000,
                             seek_step=1)

# Audio chunks generator
def export_chunks(chunks_list, path):
    export_log = []
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
        export_log.append(full_path)
    return export_log

# Speech to text conversion
def chunk_to_text(audio):
    r = sr.Recognizer()
    with audio as source:
        rec = r.record(source)
        try:
            text = r.recognize_google(rec, language='pt-BR') + ' '
        except:
            text = 'recog_failed '
    return text

def audiofile_to_text(file_number, total_files, file_path):
    os.mkdir('temp_chunks')
    audio = export_corrected_wav(file_path)
    mono_audio = split_channels(audio)
    chunks = split_on_silence(mono_audio)
    export_logs = export_chunks(chunks, 'temp_chunks')
    text = ''
    total_size = sum(os.path.getsize('temp_chunks/' + file) for file in os.listdir('temp_chunks')) / 1e+6
    incr_size_chunk = 0
    for path in export_logs:
        try:
            audio = sr.AudioFile(path)
            text += chunk_to_text(audio)
            incr_size_chunk += os.path.getsize(path) / 1e+6
            done = (incr_size_chunk / total_size) * 100
            log_str_success = '[{:.2f}%] [file {} of {}] Chunk {} conversion successfully done.'
            log_str_fail = '[{:.2f}%] [file {} of {}] Chunk {} conversion successfully FAILED!.'
            log_time = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime())
            print(log_time, log_str_success.format(done, file_number, total_files, path))
        except:
            log_time = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime())
            print(log_time, log_str_fail.format(done, file_number, total_files, path))
    shutil.rmtree("temp_chunks")
    return text

def get_files_metadata(csv_file_path):
    except_columns = ['rowPess', 'rkOcorr', 'DATAHORA', 'Date', 'UnkId',
                     'Path', 'Folder', 'MaxDateTime', 'MinReclDtOcorrencia']
    metadata = pd.read_csv('resources/lista_arquivos_mar.csv', sep=';')
    metadata = metadata.fillna(0)
    metadata = metadata.drop(except_columns, axis=1)
    return metadata

def get_total_size(audios_root_path, subfolders):
    total_size = 0
    total_files = 0
    for folder in subfolders:
        path = audios_root_path + '/' + folder
        total_size += sum(os.path.getsize(path + '/' + file) for file in os.listdir(path)) / 1e+6
        total_files += len(os.listdir(path))
    print(f'Number of files to convert: {total_files}.')
    print(f'Total size to convert: {total_size:.2f} Mb. ')
    return total_size, total_files

def load_into_json(file_name, data_to_load):
    f = open(file_name, 'w')
    json.dump(obj=data_to_load, fp=f, ensure_ascii=False) #.encode('iso-8859-1')
    f.close()

def conversion_pipeline(audios_root_path, csv_file_path):
    start_time = time.time()
    print('------------ SPEECH TO TEXT PROCESS STARTED! ------------')
    metadata = get_files_metadata(csv_file_path)
    subfolders = os.listdir(audios_root_path)
    total_size, total_files = get_total_size(audios_root_path, subfolders)
    incremental_size = 0
    files_count = 1
    data = []
    for folder in subfolders:
        folder_path = audios_root_path + '/' + folder
        files_list = os.listdir(folder_path)
        for file in files_list:
            print(f'Starting file {files_count} of {total_files} conversion ({file})...')
            file_path = folder_path + '/' + file
            try:
                file_metadata = metadata[metadata['fileName'] == file].drop(['fileName'], axis=1).iloc[:, 1:-1]
                file_metadata = file_metadata.to_dict(orient='list')
                text = audiofile_to_text(files_count, total_files, file_path)
                data.append({'file': file, 'file_metadata': file_metadata, 'audio_transcription': text})
                incremental_size += os.path.getsize(file_path) / 1e+6
                done = (incremental_size / total_size) * 100
                conv_log = ' File {} of {} successfully converted. {:.2f} Total Mb processed. {:.2f}% Overall done.'.format(
                    files_count, total_files, incremental_size, done)
                print(time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime()) + conv_log)
                files_count += 1
            except Exception:
                traceback.print_exc()
        load_into_json('data.json', data)
    print('------------ SPEECH TO TEXT PROCESS DONE! ------------')
    end_time = time.time()
    elapsed = (end_time - start_time) / 60
    avg_elapsed = elapsed / total_files
    print(f'Elapsed time: {elapsed:.2f} minutes. Avg processing time per file: {avg_elapsed:.2f} minutes.')
conversion_pipeline('audios', 'resources/lista_arquivos_mar.csv')