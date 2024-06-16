import i18next from 'i18next'
import { FormSomethingNew } from './FormSomethingNew'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', FormSomethingNew.name, en)
i18next.addResourceBundle('ru', FormSomethingNew.name, ru)

export * from './FormSomethingNew'
