import streamlit as st

st.write('Пройдите регистрацию')
name = st.text_input("Фамилия, Имя, Отчество")
job = st.text_input("Должность")

if st.button('Завершите регистрацию'):
    if name and job:
        st.write("Ф.И.О.:", name)
        st.write("Должность:", job)
        st.success('Регистрация завершена')
        st.session_state['user'] = name
        st.success(f"Текущий пользователь: {st.session_state['user']}")
    else:
        st.warning('Заполните до конца Ф.И.О. или должность')