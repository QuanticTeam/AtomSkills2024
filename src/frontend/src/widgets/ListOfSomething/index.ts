import i18next from 'i18next'
import { ListOfSomething } from './ListOfSomething'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', ListOfSomething.name, en)
i18next.addResourceBundle('ru', ListOfSomething.name, ru)

export * from './ListOfSomething'
