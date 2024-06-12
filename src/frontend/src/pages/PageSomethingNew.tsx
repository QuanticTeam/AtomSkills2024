import { zodResolver } from '@hookform/resolvers/zod'
import { Alert, BreadcrumbProps, Button, Form, Input, Popover, Typography } from 'antd'
import { t } from 'i18next'
import { Controller, SubmitHandler, useForm } from 'react-hook-form'
import { useTranslation } from 'react-i18next'
import { NavLink } from 'react-router-dom'
import { z } from 'zod'
import { PageAuthorized } from '~/layouts/PageAuthorized'
import { ROUTE_PATH_SOMETHING, ROUTE_PATH_SOMETHING_NEW } from '~/shared/routing'

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

const schema = z.object({
  name: z.string(),
  number: z.string(), // TODO parsefloat
  integer: z.string(), // TODO parseint
  dateTime: z.string(), // TODO ISO string validation
  file: z.string(), // TODO ??
})

type Data = z.infer<typeof schema>

export default function PageSomethingNew() {
  const { t } = useTranslation()

  const {
    handleSubmit,
    control,
    formState: { errors },
  } = useForm<Data>({
    values: {
      name: '',
      number: '',
      integer: '',
      dateTime: '',
      file: '',
    },
    resolver: zodResolver(schema),
  })

  const onSubmit: SubmitHandler<Data> = async data => {}

  return (
    <PageAuthorized
      breadcrumbs={breadcrumbs}
      title={t('Something new')}
    >
      <div className="w-4/5 m-auto flex items-center h-full">
        <Form
          className="grow"
          labelCol={{ span: 3, offset: 5 }}
          wrapperCol={{ span: 8 }}
          layout="horizontal"
          onFinish={handleSubmit(onSubmit)}
        >
          <Controller
            name="name"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label={t('Name')}
                help={errors.name?.message}
                validateStatus={errors.name?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder={t('Enter name')}
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />
          <Controller
            name="number"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label={t('Decimal')}
                help={errors.number?.message}
                validateStatus={errors.number?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder={t('Enter value')}
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />
          <Controller
            name="integer"
            control={control}
            render={({ field }) => (
              <Form.Item
                required
                label={t('Integer')}
                help={errors.integer?.message}
                validateStatus={errors.integer?.message && 'error'}
              >
                <Popover
                  overlayClassName="max-w-prose"
                  content={
                    <Alert
                      showIcon
                      message={
                        <Typography.Text className="text-xs text-slate-500">
                          Lorem ipsum dolor, sit amet consectetur adipisicing elit. Quaerat magnam
                          dolore possimus repellat. Numquam sapiente, modi voluptas nihil explicabo
                          cum nulla enim sed fugiat, provident ducimus doloremque cumque praesentium
                          officiis.
                        </Typography.Text>
                      }
                    />
                  }
                  trigger="focus"
                  placement="right"
                >
                  <div>
                    <Input
                      placeholder={t('Enter value')}
                      {...field}
                    />
                  </div>
                </Popover>
              </Form.Item>
            )}
          />

          <Form.Item
            label=" "
            colon={false}
          >
            <Button
              type="primary"
              htmlType="submit"
              block
            >
              {t('Create')}
            </Button>
          </Form.Item>
        </Form>
      </div>
    </PageAuthorized>
  )
}

// public record CreateSomethingRequest
// {
//     public string Name { get; set; }

//     public double Number { get; set; }

//     public int Integer { get; set; }

//     public DateTime DateTime { get; set; }

//     public IFormFile File { get; set; }
// }
