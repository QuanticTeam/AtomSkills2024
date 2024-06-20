import streamlit as st
import os
import requests

session = requests.Session()

from PIL import Image


st.subheader("Обработка данных аудиоконтроля")
uploaded_files = st.file_uploader("Upload Audio", type=["mp3", "wav", "m4a"],
                                  accept_multiple_files=True)
if uploaded_files is not None:
    # TO See details

    for image_file in uploaded_files:
        files = []
        file_details = {"filename": image_file.name, "filetype": image_file.type,
                        "filesize": image_file.size}

        # Saving upload
        try:
            with open(os.path.join(image_file.name), "wb") as f:
                f.write((image_file).getbuffer())
            files.append(('files', open(image_file.name, 'rb')))

            url = 'http://127.0.0.1:8000/audio_defects_with_proba'
            resp = requests.post(url=url, files=files).json()
            print(resp)
            file_details['Наличие брака'] = resp['message'][image_file.name]['Дефект']
            file_details['Вероятность текущего дефекта или его отсутствия'] = resp['message'][image_file.name]['Вероятность текущего дефекта и иных']

            st.write(f'Файл: {image_file.name}')
            if resp['message'][image_file.name]['Дефект'] == 'стук по металлу без дефекта':
                st.success('Дефект отсутствует. Обнаружен стук по металлу без дефекта')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message'][image_file.name]['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message'][image_file.name]['Дефект'] == 'стук не по металлу':
                st.warn('Обнаружен брак аудио - стук не по металлу')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message'][image_file.name]['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message'][image_file.name]['Дефект'] == 'обычный акустический шум':
                st.warn('Обнаружен брак аудио - обычный акустический шум')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message'][image_file.name]['Вероятность текущего дефекта и иных']:
                    st.info(proba)
            elif resp['message'][image_file.name]['Дефект'] == 'стук по металлу с дефектом':
                st.error('Обнаружен ДЕФЕКТ - стук по металлу с дефектом')
                st.subheader('Вероятность прогнозирования')
                for proba in resp['message'][image_file.name]['Вероятность текущего дефекта и иных']:
                    st.info(proba)

        except Exception:
            st.error('Невозможно обработать аудио. Попробуйте загрузить другой файл')

