import { BreadcrumbProps } from 'antd'
import { t } from 'i18next'
import { SubmitHandler } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { NavLink, useNavigate } from 'react-router-dom'
import { SomethingNewDto } from '~/entities/Something'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { apiClient } from '~/shared/apiClient'
import { ROUTE_PATH_SOMETHING, ROUTE_PATH_SOMETHING_NEW } from '~/shared/routing'
import { FormSomethingNew } from '~/widgets/FormSomethingNew/FormSomethingNew'

const breadcrumbs: BreadcrumbProps['items'] = [
  {
    title: (
      <NavLink
        to={ROUTE_PATH_SOMETHING}
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        {t('All entities')}
      </NavLink>
    ),
  },
  {
    title: (
      <NavLink
        to={ROUTE_PATH_SOMETHING_NEW}
        className={({ isActive }) => (!isActive ? '!text-link-1' : '')}
        end
      >
        {t('New entity')}
      </NavLink>
    ),
  },
]

export default function PageSomethingNew() {
  const { t } = useTranslation()
  const navigate = useNavigate()

  const onSubmit: SubmitHandler<SomethingNewDto> = async dto => {
    await apiClient.post('/Something/Create', dto)
  }

  return (
    <PageAuthorized
      breadcrumbs={breadcrumbs}
      title={t('Something new')}
    >
      <FormSomethingNew
        onSubmit={async (...args) => {
          await onSubmit(...args) // TODO handle errors
          navigate(ROUTE_PATH_SOMETHING)
        }}
        children={null}
      />
    </PageAuthorized>
  )
}
