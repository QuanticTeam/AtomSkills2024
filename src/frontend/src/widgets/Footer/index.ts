import i18next from 'i18next'
import { Footer } from './Footer'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', Footer.name, en)
i18next.addResourceBundle('ru', Footer.name, ru)

export * from './Footer'
