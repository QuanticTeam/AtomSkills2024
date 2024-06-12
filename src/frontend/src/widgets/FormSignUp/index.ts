import i18next from 'i18next'
import { FormSignUp } from './FormSignUp'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', FormSignUp.name, en)
i18next.addResourceBundle('ru', FormSignUp.name, ru)

export * from './FormSignUp'
