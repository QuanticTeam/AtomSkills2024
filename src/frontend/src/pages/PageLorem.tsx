import { faker } from '@faker-js/faker'
import { Button, notification } from 'antd'
import { AxiosError } from 'axios'
import { capitalize } from 'lodash'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { httpClient } from '~/shared/httpClient'

const breadcrumbs = [
  { title: 'Header left' },
  ...Array.from({ length: 2 }, () => ({ title: capitalize(faker.hacker.noun()) })),
]
export default function PageLorem() {
  const [notificationApi, contextHolder] = notification.useNotification()

  return (
    <PageAuthorized
      title="Page title"
      breadcrumbs={breadcrumbs}
    >
      {contextHolder}

      <div>
        <p>
          <Button
            type="primary"
            onClick={async () => {
              try {
                const result = await httpClient.post(`/Something/Test`)

                notificationApi.success({
                  message: 'Request OK',
                  description: result.data,
                })
              } catch (e) {
                if (e instanceof AxiosError) {
                  notificationApi.error({
                    message: e.message,
                    description: <code className="text-xs">{JSON.stringify(e.response)}</code>,
                  })
                }
              }
            }}
          >
            Backend test request
          </Button>
        </p>
        <br />
        <p>{faker.lorem.sentence(1000)}</p>
      </div>
    </PageAuthorized>
  )
}
