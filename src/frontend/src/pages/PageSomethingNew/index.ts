import i18next from 'i18next'
import { PageSomethingNew } from './PageSomethingNew'

import en from './localization/en.json'
import ru from './localization/ru.json'

i18next.addResourceBundle('en', PageSomethingNew.name, en)
i18next.addResourceBundle('ru', PageSomethingNew.name, ru)

export default PageSomethingNew
