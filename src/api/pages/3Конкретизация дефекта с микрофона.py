import streamlit as st
from streamlit_mic_recorder import mic_recorder, speech_to_text
import traceback
import requests

state = st.session_state

if 'text_received' not in state:
    state.text_received = []

c1, c2 = st.columns(2)
with c1:
    st.write("Создать файл аудио контроля с использованием микрофона:")
with c2:
    text, audio = speech_to_text(language='ru', use_container_width=True, just_once=True, key='STT')

if audio:
    if text:
        st.error('На аудио обнаружена речь, файл аудио контроля отбракован')

    else:
        st.success('Входной контроль пройден')
        st.audio(audio['bytes'])
        
        with open('temp.wav', 'wb') as f:
            f.write(audio['bytes'])
            
               # Saving upload
        try:
            
            files = []
            files.append(('files', open('temp.wav', 'rb')))

            url = 'http://172.20.1.41:8000/audio_defects_with_proba'
            resp = requests.post(url=url, files=files).json()
            print(resp)

            st.write(f'Файл: {'temp.wav'}')
            if resp['message']['temp.wav']['Дефект'] == 'стук по металлу без дефекта':
                st.success('Дефект отсутствует. Обнаружен стук по металлу без дефекта')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message']['temp.wav']['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message']['temp.wav']['Дефект'] == 'стук не по металлу':
                st.warn('Обнаружен брак аудио - стук не по металлу')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message']['temp.wav']['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message']['temp.wav']['Дефект'] == 'обычный акустический шум':
                st.warn('Обнаружен брак аудио - обычный акустический шум')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message']['temp.wav']['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message']['temp.wav']['Дефект'] == 'стук по металлу с дефектом':
                st.error('Обнаружен ДЕФЕКТ - стук по металлу с дефектом')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message']['temp.wav']['Вероятность текущего дефекта и иных']:
                    st.info(proba)

        except Exception:
            print(traceback.format_exc())
            st.error('Невозможно обработать аудио. Попробуйте загрузить другой файл')