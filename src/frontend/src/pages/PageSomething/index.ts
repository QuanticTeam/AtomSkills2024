import i18next from 'i18next'
import { PageSomething } from './PageSomething'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageSomething.name, en)
i18next.addResourceBundle('ru', PageSomething.name, ru)

export default PageSomething
