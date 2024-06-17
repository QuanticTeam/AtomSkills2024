import i18next from 'i18next'
import { initReactI18next } from 'react-i18next'

export function getLocale() {
  return localStorage.getItem('locale') || 'en-US'
}

export function setLocale(locale: string) {
  localStorage.setItem('locale', locale)
}

// setLocale('ru-RU')
setLocale('en-US')

i18next.use(initReactI18next).init({
  lng: getLocale(),

  // Disable default escaping, which prevents XSS attacks. React already takes care of this.
  interpolation: {
    escapeValue: false,
  },
})
