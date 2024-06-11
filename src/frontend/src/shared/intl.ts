import i18next from 'i18next'
import { initReactI18next } from 'react-i18next'

i18next.use(initReactI18next).init({
  resources: {
    'en-US': {
      translation: {
        'Forgot password': 'Forgot password',
        Login: 'Login',
        Password: 'Password',
        Remember: 'Remember me',
        Role: 'Role',
        'Sign in': 'Sign in',
        'Sign up': 'Sign up',
        'Successfuly authorized': 'Successfuly authorized',
      },
    },
    'ru-RU': {
      translation: {
        'Forgot password': 'Не помню пароль',
        Login: 'Логин',
        Password: 'Пароль',
        Remember: 'Запомнить меня',
        Role: 'Роль',
        'Sign in': 'Войти',
        'Sign up': 'Зарегистрироваться',
        'Successfuly authorized': 'Вы успешно зарегистрировались',
      },
    },
  },
  lng: 'en-US',
  // Disable default escaping, which prevents XSS attacks. React already takes care of this.
  interpolation: {
    escapeValue: false,
  },
})
