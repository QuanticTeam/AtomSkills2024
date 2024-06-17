import { Button } from 'antd'
import { useTranslation } from 'react-i18next'
import { Link } from 'react-router-dom'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { ROUTE_PATH_SOMETHING_NEW } from '~/shared/routing'
import { ListOfSomething } from '~/widgets'

export function PageSomething() {
  const { t } = useTranslation(PageSomething.name)

  return (
    <PageAuthorized
      actions={[
        {
          key: 'create',
          node: (
            <Link to={ROUTE_PATH_SOMETHING_NEW}>
              <Button type="primary">{t('create')}</Button>
            </Link>
          ),
        },
      ]}
      title={t('title')}
    >
      <ListOfSomething />
    </PageAuthorized>
  )
}
