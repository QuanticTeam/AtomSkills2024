import i18next from 'i18next'
import { HeaderAuthorized } from './HeaderAuthorized'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', HeaderAuthorized.name, en)
i18next.addResourceBundle('ru', HeaderAuthorized.name, ru)

export * from './HeaderAuthorized'
