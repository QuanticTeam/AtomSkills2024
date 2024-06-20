from fastapi import FastAPI, Request
from fastapi import File, UploadFile
from typing import List
import traceback
import numpy as np
import matplotlib.pyplot as plt
import cv2
from fastapi.responses import FileResponse
import pickle
import imutils
import librosa
from sklearn.metrics import f1_score

from sklearn.neural_network import MLPClassifier

label_dict = {0: 'Нет дефекта', 1:'Прожог', 2:'Брызги',3:'Шлаковые',4:'Непровар',5:'Наплыв',6:'Подрез', 7:'Трещина', 8:'Поры', 9:'Кратер', 10:'Ассиметрия'}
label_dict_reverse = {v:k for k,v in label_dict.items()}
label_dict_audio = {0: 'стук по металлу без дефекта', 1:'стук по металлу с дефектом', 2:'стук не по металлу',3:'обычный акустический шум'}

recommend = {'Прожог': {'причины': ['Недостаточное удаление шлака предыдущего слоя',
                                    'Заниженный режим сварки, недостаточное тепловложение',
                                    'Плохая защита шва при сварке в среде защитных газов',
                                    'Слишком узкая разделка, увеличить угол раскрытия',
                                    ],
                        'ремонт': ['Выбрать угловой шлифовальной машинкой до чистого металла и подварить'
                                   ]
                        },
             'Брызги': {'причины': ['Длинная дуга / высокое напряжение',
                                    'Неправильные параметры сварки',
                                    'Влажные, грязные, поврежденные электроды',
                                    'Загрязнение кромок разделки или не соответствие компонента защитного газа или смеси присадочных материалов, например, ржавчиной',
                                    ],
                        'предотвращение': ['Отрегулируйте параметры сварки',
                                           'Варите короткой дугой / снизьте напряжение',
                                           'Варите хорошими прокаленными электродами',
                                           'Зачищайте кромки разделки и присадочные материалы',
                                           'Передвинуть зажим заземления, варить короткой дугой, снизить сварочный ток, наклонить электрод в сторону, противоположную направлению магнитного дутья, вести сварку на переменном токе',
                                           'Переключить полярность'
                                           ],
                        'ремонт': ['Удаление брызг с помощью зубила, шабера',
                                   'Удаление брызг при зачистке шлифовальной машинкой'
                                   ]
                        },
             'Шлаковые': {'причины': ['Недостаточное удаление шлака предыдущего слоя',
                                      'Заниженный режим сварки, недостаточное тепловложение',
                                      'Плохая защита шва при сварке в среде защитных газов',
                                      'Слишком узкая разделка, увеличить угол раскрытия',
                                      ],
                          'ремонт': ['Выбрать угловой шлифовальной машинкой до чистого металла и подварить'
                                     ]
                          },
             'Непровар': {'причины': ['Неправильная сборка деталей или конструктив шва',
                                      'Притупление кромок, зазор, режим сварки и т.п. ',
                                      ],
                          'ремонт': [
                              'Выбрать наплавленный металл, разрезать, зачистить, заново собрать и заварить до чистого металла, угловой шлифовальной машинкой и заново заварить'
                              ]
                          },

             'Наплыв': {'причины': ['Завышенные режимы сварки',
                                    'Неудобное пространственное положение сварки или наклона плоскости на которую накладывается шов',
                                    ],
                        'ремонт': ['Зачистка угловой шлифовальной машинкой'
                                   ]
                        },

             'Подрез': {'причины': ['Неправильный угол наклона электрода',
                                    'Недостаточное заполнение разделки под облицовку',
                                    'Завышенные режимы сварки',
                                    ],
                        'ремонт': ['Подварка',
                                   'Наплавка дополнительного валика-усиления'
                                   ]
                        },

             'Трещина': {'причины': [
                 'Не правильные пропорции химического состава основного металла и сварочных материалов – необходимо использовать сварочные материалы соответствующие основному металлу свариваемых заготовок',
                 'Разная температура плавления соединяемых деталей – выполнять предварительный подогрев заготовок',
                 'Быстрое охлаждение детали после сварки',
                 'Включения посторонних веществ'
                 ],
                         'ремонт': [
                             'Зачистка-выборка до чистого металла, угловой шлифовальной машинкой и заново заварить',
                             ]
                         },

             'Поры': {'причины': ['Неочищенная поверхность кромок',
                                  'Использование влажных флюсов или электродов',
                                  'Плохая защита шва при сварке в среде защитных газов  ',
                                  ],
                      'ремонт': ['Выбрать угловой шлифовальной машинкой до чистого металла',
                                 ]
                      },

             'Кратер': {'причины': ['Резкая остановка сварки',
                                    'Из-за мгновенной кристаллизации металла',
                                    'Установить в настройках аппарата плавное гашение дуги или заварку кратера',
                                    ],
                        'ремонт': ['Зачистка, угловой шлифовальной машинкой и заварить',
                                   ]
                        },

             'Ассиметрия': {'причины': ['Неочищенная поверхность кромок',
                                    'Быстрое охлаждение детали после сварки',
                                    ],
                        'ремонт': ['Зачистка, угловой шлифовальной машинкой и заварить',
                                   ]
                        },

             'Нет дефекта': {'причины': ['Не требуется'],
                             'ремонт': ['Не требуется']
                             },
             }

app = FastAPI()


def check_qual(img):
    size = img.shape[0]
    check = img.mean(axis=1).mean(axis=1)
    result = len(np.where(check < 30)[0]) + len(np.where(check > 230)[0])

    return (result / size) > 0.1


def equalization_and_denoise(image_path):
    rgb_img = cv2.imread(image_path)

    # convert from RGB color-space to YCrCb
    ycrcb_img = cv2.cvtColor(rgb_img, cv2.COLOR_BGR2YCrCb)

    # equalize the histogram of the Y channel
    ycrcb_img[:, :, 0] = cv2.equalizeHist(ycrcb_img[:, :, 0])

    # convert back to RGB color-space from YCrCb
    equalized_img = cv2.cvtColor(ycrcb_img, cv2.COLOR_YCrCb2BGR)

    # удаление шума и сглаживание изображения
    dst = cv2.fastNlMeansDenoisingColored(equalized_img, None, 10, 10, 7, 21)  #удаление шума

    kernel = np.ones((5, 5), np.float32) / 25
    dst = cv2.filter2D(dst, -1, kernel)  #сглаживание изображения

    return equalized_img


def image_to_feature_vector(image, size=(32, 32)):
    # resize the image to a fixed size, then flatten the image into
    # a list of raw pixel intensities
    return cv2.resize(image, size).flatten()


def extract_color_histogram(image, bins=(8, 8, 8)):
    # extract a 3D color histogram from the HSV color space using
    # the supplied number of `bins` per channel
    hsv = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)
    hist = cv2.calcHist([hsv], [0, 1, 2], None, bins,
                        [0, 180, 0, 256, 0, 256])
    # handle normalizing the histogram if we are using OpenCV 2.4.X
    if imutils.is_cv2():
        hist = cv2.normalize(hist)
    # otherwise, perform "in place" normalization in OpenCV 3 (I
    # personally hate the way this is done
    else:
        cv2.normalize(hist, hist)
    # return the flattened histogram as the feature vector
    return hist.flatten()


def preprocess_audio(audio_file_path):
    signal, sr = librosa.load(audio_file_path)

    mfcc_mean = np.mean(librosa.feature.mfcc(y=signal, sr=sr), axis=1).mean()
    mfcc_std = np.std(librosa.feature.mfcc(y=signal, sr=sr), axis=1).std()

    cent = librosa.feature.spectral_centroid(y=signal, sr=sr)
    cent_mean = np.mean(cent)
    cent_std = np.std(cent)

    rolloff = librosa.feature.spectral_rolloff(y=signal, sr=sr)
    roloff_mean = np.mean(rolloff)
    roloff_std = np.std(rolloff)

    zrate = librosa.feature.zero_crossing_rate(signal)
    zrate_mean = np.mean(zrate)
    zrate_std = np.std(zrate)

    return mfcc_mean, mfcc_std, cent_mean, cent_std, roloff_mean, roloff_std, zrate_mean, zrate_std

# app = FastAPI()


@app.get("/items_root")
def read_root():
    return {"Привет": "Я - API. Помогу вам дефекты швов"}


@app.post("/upload_images")
def upload_images(files: List[UploadFile] = File(...)):
    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\load_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)
        except Exception:
            print(traceback.format_exc())
            return {"message": "There was an error uploading the file(s)"}
        finally:
            file.file.close()

    return {"message": f"Successfuly uploaded {[file.filename for file in files]}"}


@app.post("/upload_audio")
def upload_audio(files: List[UploadFile] = File(...)):
    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\audio_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)
        except Exception:
            print(traceback.format_exc())
            return {"message": "There was an error uploading the file(s)"}
        finally:
            file.file.close()

    return {"message": f"Successfuly uploaded {[file.filename for file in files]}"}


@app.post("/check_qual_images")
def check_qual_images(files: List[UploadFile] = File(...)):
    result = {}
    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)
            img = plt.imread(f'.\\temp_from_api\\{file.filename}')
            check = check_qual(img)
            if check:
                result[file.filename] = 'Некачественная фотография'
            else:
                result[file.filename] = 'Качественная фотография'
        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error check the file"
        finally:
            file.file.close()

    return {"message": result}


@app.post("/process_images")
def process_images(files: List[UploadFile] = File(...)):
    result = {}
    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)
            equalized_img = equalization_and_denoise(
                f'.\\temp_from_api\\{file.filename}')
            plt.imsave(f'.\\temp_from_api\\{file.filename}', equalized_img)
            return FileResponse(path=f'.\\temp_from_api\\{file.filename}',
                                filename=file.filename, media_type='multipart/form-data')
        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error check the file"
        finally:
            file.file.close()

    return {"message": result}


@app.post("/binary_classification")
def binary_classification(files: List[UploadFile] = File(...)):
    with open('.\\final_mlp.pkl', 'rb') as f:
        model = pickle.load(f)

    result = {}
    for file in files:

        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            equalized_img = equalization_and_denoise(f'.\\temp_from_api\\{file.filename}')
            hist = extract_color_histogram(equalized_img)

            pred = label_dict[model.predict(hist.reshape(1, -1))[0]]

            if pred == '0':
                pred = 'Нет дефекта'
            else:
                pred = 'Есть дефект'

            result[file.filename] = pred

        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error classify the file"
        finally:
            file.file.close()

    return {"message": result}


@app.post("/detect_defect")
def detect_defect(files: List[UploadFile] = File(...)):
    with open('.\\final_mlp.pkl', 'rb') as f:
        model = pickle.load(f)

    result = {}
    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            equalized_img = equalization_and_denoise(f'.\\temp_from_api\\{file.filename}')
            hist = extract_color_histogram(equalized_img)

            pred = label_dict[model.predict(hist.reshape(1, -1))[0]]

            if pred == '0':
                pred = 'Нет дефекта'
                result[file.filename] = {'label': pred, 'sx':None, 'sy':None, 'ex':None, 'ey':None}
            else:
                result[file.filename] = {'label': pred, 'sx':np.random.random(), 'sy':np.random.random(), 'ex':np.random.random(), 'ey':np.random.random()}

        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error classify the file"
        finally:
            file.file.close()

    return {"message": result}


@app.post("/classification_defects")
def classification_defects(files: List[UploadFile] = File(...)):
    with open('.\\final_mlp.pkl', 'rb') as f:
        model = pickle.load(f)

    result = {}
    for file in files:

        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            equalized_img = equalization_and_denoise(
                f'.\\temp_from_api\\{file.filename}')
            hist = extract_color_histogram(equalized_img)

            pred = label_dict[model.predict(hist.reshape(1, -1))[0]]

            if pred == '0':
                pred = 'Нет дефекта'

            result[file.filename] = pred

        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error classify the file"
        finally:
            file.file.close()
    print(result)
    return {"message": result}

@app.post("/classification_defects_with_proba")
def classification_defects_with_proba(files: List[UploadFile] = File(...)):
    with open('.\\final_mlp.pkl', 'rb') as f:
        model = pickle.load(f)

    with open('.\\bound_model.pkl', 'rb') as f:
        bbox_model = pickle.load(f)


    for file in files:
        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            equalized_img = equalization_and_denoise(
                f'.\\temp_from_api\\{file.filename}')
            hist = extract_color_histogram(equalized_img)

            pred = label_dict[model.predict(hist.reshape(1, -1))[0]]

            proba = model.predict_proba(hist.reshape(1, -1))[0]

            proba_result = []
            
            result = {}
            
            for keys in label_dict.keys():
                try:
                    proba_result.append(f'{label_dict[keys]}: {round(proba[keys], 6) * 100}%')
                except Exception:
                    continue

            if pred == '0':
                pred = 'Нет дефекта'

            result['features'] = [pred]
            result['probability'] = proba_result
            result['recomendation'] = recommend[pred]
            
            if pred == 'Нет дефекта':
                return []
            else:
                bbox = bbox_model.predict(hist.reshape(1, -1))[0]
                print(bbox)
                result['area'] = {'x1': bbox[0], 'y1': bbox[1], 'x2': bbox[2], 'y2': bbox[3]}
        except Exception:
            print(traceback.format_exc())
            result = {'features': ["There was an error classify the file"], 'probability': [], 'recomendation': {}, 'area': {}}
        finally:
            file.file.close()
    print(result)
    return [result]

@app.post("/audio_defects")
def audio_defects(files: List[UploadFile] = File(...)):

    with open('.\\final_knn.pkl', 'rb') as f:
        model = pickle.load(f)

    result = {}
    for file in files:

        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            audio_features = preprocess_audio(f'.\\temp_from_api\\{file.filename}')
            print(audio_features)
            pred = label_dict_audio[model.predict(np.array(audio_features).reshape(1, -1))[0]]


            result[file.filename] = pred

        except Exception:
            print(traceback.format_exc())
            result[file.filename] = "There was an error classify the file"
        finally:
            file.file.close()

    return {"message": result}

@app.post("/audio_defects_with_proba")
def audio_defects(files: List[UploadFile] = File(...)):

    with open('.\\final_knn.pkl', 'rb') as f:
        model = pickle.load(f)

    result = {}
    for file in files:

        try:
            contents = file.file.read()
            with open(f'.\\temp_from_api\\{file.filename}', 'wb') as f:
                f.write(contents)

            audio_features = preprocess_audio(f'.\\temp_from_api\\{file.filename}')
            print(audio_features)
            pred = label_dict_audio[model.predict(np.array(audio_features).reshape(1, -1))[0]]
            proba = model.predict_proba(np.array(audio_features).reshape(1, -1))[0]

            proba_result = []

            for keys in label_dict_audio.keys():
                try:
                    proba_result.append(f'{label_dict_audio[keys]}: {round(proba[keys], 6) * 100}%')
                except Exception:
                    continue

            if pred == '0':
                pred = 'Нет дефекта'

            result[file.filename] = {'Дефект': pred, 'Вероятность текущего дефекта и иных': proba_result}

        except Exception:
            print(traceback.format_exc())
            result[file.filename] = {'Дефект':"There was an error classify the file", 'Вероятность текущего дефекта и иных': None}
        finally:
            file.file.close()

    return {"message": result}

@app.post("/calc_qual")
async def calc_qual(request: Request):
    result = {}
    try:
        data = await request.json()
    except Exception:
        print(traceback.format_exc())
        return {"message": 'Not enough data or data is corrupted'}

    y_true = []
    y_hat = []

    try:

        for obj in data.keys():
            y_true.append(data[obj]['y_true'])
            y_hat.append(data[obj]['y_hat'])
            result[obj] = data[obj]['y_true'] == data[obj]['y_hat']

        f1 = f1_score(y_true, y_hat, average='weighted')
        result['f1'] = f1

        return result

    except Exception:
        print(traceback.format_exc())
        return {"message": "There was an error calc qual. Check data"}

@app.post("/get_recommend")
async def get_recommend(request: Request):
    result = {}
    try:
        data = await request.json()
    except Exception:
        print(traceback.format_exc())
        return {"message": 'Not enough data or data is corrupted'}

    try:

        for obj in data.keys():
            try:
                result[obj] = recommend[data[obj]]
            except Exception:
                result[obj] = 'Ошибка получения рекомендации'

        return result

    except Exception:
        print(traceback.format_exc())
        return {"message": "There was an error calc qual. Check data"}