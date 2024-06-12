import i18next from 'i18next'
import { FormSignIn } from './FormSignIn'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', FormSignIn.name, en)
i18next.addResourceBundle('ru', FormSignIn.name, ru)

export * from './FormSignIn'
